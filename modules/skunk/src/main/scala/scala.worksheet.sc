
import skunk.Decoder
import skunk.data.Type
import cats.syntax.all._

sealed trait NullabilityKnown
object NullabilityKnown {
  case object NoNulls extends NullabilityKnown
  case object Nullable extends NullabilityKnown
}
import NullabilityKnown._

case class Row(elems: List[Any]) {
  def apply(i: Int): Any = elems(i)
}

object Row {

  // Placeholder for nulls read from non-nullable columns introduced via an outer join.
  case object FailedJoin

  def mkDecoder(metas: List[(Boolean, (Decoder[_], NullabilityKnown))]): Decoder[Row] =
    new Decoder[Row] {

      lazy val types: List[Type] = metas.flatMap { case (_, (d, _)) => d.types }

      lazy val decodersWithOffsets: List[(Boolean, Decoder[_], NullabilityKnown, Int)] =
        metas.foldLeft((0, List.empty[(Boolean, Decoder[_], NullabilityKnown, Int)])) {
          case ((offset, accum), (isJoin, (decoder, nullity))) =>
            (offset + decoder.length, (isJoin, decoder, nullity, offset) :: accum)
        } ._2.reverse

      def decode(start: Int, ss: List[Option[String]]): Either[Decoder.Error,Row] =
        decodersWithOffsets.traverse {

          // If the column is the outer part of a join and it's a non-nullable in the schema then
          // we read it as an option and collapse it, using FailedJoin in the None case. Otherwise
          // read as normal.
          case (true, decoder, NoNulls, offset) => decoder.opt.decode(start + offset, ss).map(_.getOrElse(FailedJoin))
          case (_,    decoder, _,       offset) => decoder    .decode(start + offset, ss)

        } .map(Row(_))

    }

}

